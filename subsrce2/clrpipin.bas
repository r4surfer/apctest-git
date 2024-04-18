        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   CCC   L      RRRR   PPPP   IIIII  PPPP   IIIII  N   N   *~
            *  C   C  L      R   R  P   P    I    P   P    I    NN  N   *~
            *  C      L      RRRR   PPPP     I    PPPP     I    N N N   *~
            *  C   C  L      R   R  P        I    P        I    N  NN   *~
            *   CCC   LLLLL  R   R  P      IIIII  P      IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CLRPIPIN - SUBROUTINE USED TO CLEAR AN ENTRY IN THE PIPIN *~
            *            AND ITS DEMAND/WORK CENTER STUFF.              *~
            *            ONLY PLANNED PIPIN'S (WO/BO) CAN BE CLEARED.   *~
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
            * 07/05/83 ! ORIGINAL                                 ! ECR *~
            * 08/19/93 ! Purchase Job Support - BW's              ! KAB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "CLRPIPIN" (                                                 ~
                     part$,                                              ~
                     today%,                                             ~
                     day%,                                               ~
                     ptagnr$,            /* TAG NO. OF PIPIN TO CLEAR  */~
                     #1,                 /* DEMMASTR                   */~
                     #8,                 /* JBCROSS2                   */~
                     #2,                 /* PIPMASTR                   */~
                     #11,                /* WCMASTR                    */~
                     #23,                /* WCOUT                      */~
                     #33,                /* PIPIN                      */~
                     #34,                /* PIPOUT                     */~
                     #41,                /* SFCUM2                     */~
                     #35,                /* PIPCROSS                   */~
                     #36)                /* JBPIPXRF                   */

        dim part$25, pip%(490),              ptagnr$19,                  ~
           pipinaltkey$48, all$100,                                      ~
           pipoutkey$56, part1$25,             wcoutkey$35, wc$4



        REM NOW FIND THE PIPIN.               CLEAR IT IF IT IS JUST A   ~
           PLAN.
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************
           if str(ptagnr$,1,2) = "WO" or               /* JUST PLANS */  ~
              str(ptagnr$,1,2) = "BW" or               /* BW = PWO   */  ~
              str(ptagnr$,1,2) = "BO" then goto L10060  /* W=JO, B=PO */  ~
                                      else goto L65000  /* ELSE GET OUT*/

L10060:    put pipinaltkey$ , using  L10070 , part$, day%, ptagnr$
L10070:    FMT CH(25), BI(4), CH(19)
           call "REDALT0" (#33, pipinaltkey$, 1%, f1%)
           if f1% <> 1 then goto  L65000   /*NO MATACHING PIPIN         */

           get #33, using L10120, ptagq
L10120:              FMT XX(29), XX(19), PD(14,4)

        REM THESE ARE PLANS THAT CAN BE CLEARED

           init(hex(00)) pipoutkey$
           str(pipoutkey$,1,19) = str(ptagnr$,1,19)
L10180:    call "PLOWNXT1" (#34, pipoutkey$,  19%, f1%)
           if f1% <> 1 then goto  L10340               /* FIX WC */
           get #34, using L10220, part1$, day1%,  outqty
                     delete #34
L10220:    FMT XX(19), CH(25), BI(4), XX(8), PD(14,4)
        call "PIPFLAGS"                                                  ~
                    (part1$,             /* THE PART TO CHECK          */~
                     today%,                                             ~
                     day1%,              /* DAY QUANTITY ADDED         */~
                     outqty,             /* QUANTITY TO ADD            */~
                     #2,                 /* PIPMASTR                   */~
                     #41)                /* SFCUM2                     */


           goto L10180

L10340:    REM NOW ELIMINATE THE WC USEAGE & DEMAND MASTER FILE RECORD

        REM CALL "REDALT1" (#1, "PC"&STR(PTAGNR$,3,17), 1%, F1%)
        REM IF F1% <> 1 THEN GOTO 10400
        REM DELETE #1

           init(hex(00)) wcoutkey$
           str(wcoutkey$,,19) = str(ptagnr$,,19)
L10420:    call "PLOWNXT1" (#23, wcoutkey$, 19%, f1%)
           if f1% <> 1 then L10590
           get #23, using L10460, wc$, day1%, setup%, run%
           outqty = setup% + run%
           delete #23
L10460:    FMT CH(4), BI(2), XX(25), 2*BI(4)
           call "READ101" (#11, wc$, f1%)
                if f1% = 0 then L10420
           get #11, using L10520, pip%()
           pip%(day1%) = pip%(day1%) - outqty
           put #11, using L10520, pip%()
L10520:    FMT POS(1040), 490*BI(2)
           rewrite #11
           goto L10420

L10590: REM NOW TO CORRECT THE PIP FOR THE PART WE STARTED WITH
           call "READ101" (#8 , ptagnr$, f1%)
           if f1% = 1% then delete #8
           call "READ101" (#33, ptagnr$, f1%)
           if f1% = 1% then delete #33
           ptagq = 0 - ptagq      /* REVERSE THE SIGN OF PTAGQ */
        call "PIPFLAGS"                                                  ~
                    (part$,              /* THE PART TO CHECK          */~
                     today%,                                             ~
                     day%,               /* DAY QUANTITY ADDED         */~
                     ptagq,              /* QUANTITY TO ADD            */~
                     #2,                 /* PIPMASTR                   */~
                     #41)                /* SFCUM2                     */

            call "REDALT1" (#36, ptagnr$, 1%, f1%)
                if f1%<>0 then delete #36
            call "DELETE" (#36, ptagnr$, 19%)

            init (hex(00)) all$ : str(all$,1,19) = str(ptagnr$,1,19)
L10820:     call "PLOWAL1" (#35, all$, 1%, 19%, f1%)
            if f1% = 0 then L10900
            delete #35
            put #35, using L10860, " CANCELLED       "
L10860:         FMT POS(22), CH(17)
            write #35
            goto L10820

L10900:     init (hex(00)) all$ : str(all$,1,19) = str(ptagnr$,1,19)
L10910:     call "PLOWAL1" (#35, all$, 2%, 19%, f1%)
            if f1% = 0 then L65000
            delete #35
            put #35, using L10950, " CANCELLED       "
L10950:         FMT POS(41), CH(17)
            write #35
            goto L10910

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
