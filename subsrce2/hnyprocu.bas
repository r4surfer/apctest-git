        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  H   N  N   N  Y   Y  PPPP   RRRR    OOO    CCC   U   U   *~
            *  H   H  NN  N   Y Y   P   P  R   R  O   O  C      U   U   *~
            *  HHHHH  N N N    Y    PPPP   RRRR   O   O  C      U   U   *~
            *  H   H  N  NN    Y    P      R R    O   O  C      U   U   *~
            *  H   H  N   N    Y    P      R  R    OOO    CCC    UUU    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYPROCU - Post Procurements to HNY Procurement Record.   *~
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
            * 07/29/83 ! ORIGINAL (PROCPOST)                      ! KEN *~
            * 05/13/87 ! Std Cost Changes (renamed HNYPROCU)      ! ERN *~
            * 06/24/94 ! Removed advice numbers from Part for     ! ERN *~
            *          !   Vendor Service records                 !     *~
            * 04/25/95 ! PRR 12026,12664,11945. Added PO#/Inv#.   ! JDH *~
            * 11/07/95 ! Minor fix for argument size mismatches   ! KAB *~
            * 09/04/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "HNYPROCU" (prt$, prcsrce$, date$, quantity, invcost,    ~
                            orddate$, #2, #3)
                                     /* #2 = HNYMASTR, #3 = SYSFILE2 */

            dim part$25, procsrce$26, procdate$8, date$6, orddate$6,     ~
                revdate$6, readkey$50, costtype$1, prt$25, prcsrce$26

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L01032
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
L01032: REM *************************************************************
            select #1 "HNYPROC",                                         ~
                      varc, indexed, recsize = 134,                      ~
                      keypos =   32, keylen =  40,                       ~
                      alt key  1, keypos =    7, keylen =  65,           ~
                          key  2, keypos =    1, keylen =  40, dup,      ~
                          key  3, keypos =   41, keylen =  31, dup


            part$ = prt$ : procsrce$ = prcsrce$
            if abs(quantity) <= .01 or procsrce$ = " " then end

            if opened% <> 0% then L10000
                call "OPENCHCK" (#1, 0%, 0%, 100%, " ")  /* HNYPROC */
                opened% = 1%

L10000: REM *************************************************************~
            *  P O S T   H N Y P R O C                                  *~
            *************************************************************

           stdcost = 0  :  costtype$ = "N"
           call "READ100" (#2, part$, f1%)   /* HNYMASTR */
           if f1% = 0% and str(part$,,10) = "ACTIVITY: " then            ~
                part$ = str(part$,,14)
           if f1% = 0% then L10100
                get #2 using L10080, costtype$
L10080:              FMT POS(307), CH(1)
                call "STCCOSTS" (part$, " ", #3, 1%, stdcost)

L10100:    call "DATREVRS" (date$, revdate$, " " )

           procqty, proccost = 0 : procdate$ = "99999999"
           call "DATECONV" (procdate$)
           readkey$ = str(procsrce$,,9) & str(part$,,25) & revdate$
           call "READ101" (#1, readkey$, f1%)
           if f1% = 0% then goto L10230
                get #1 using L10200, procqty, proccost, procdate$, temp$
L10200:              FMT POS(72), 2*PD(14,4), POS(104), CH(6), POS(118), ~
                         CH(16)
                delete #1
                if str(procsrce$,,1%) = "*" then L10230
                     if str(temp$,16%,1%) = "+" then                     ~
                           str(procsrce$,26%,1%) = "+" /* Multiple POs */
                     if str(temp$,,14%) <> str(procsrce$,,14%) then      ~
                           str(procsrce$,26%,1%) = "+" /* Multiple POs */
L10230:     pqty = round(procqty + quantity, 2%) : if pqty = 0 then end
            proccost = round((proccost*procqty+invcost*quantity)/pqty,4%)
            procqty  = pqty
            if procdate$ > orddate$ then procdate$ = orddate$

            write #1 using L10410,        /* HNYPROC                    */~
                     date$,              /* Date                       */~
                     part$,              /* Part                       */~
                     str(procsrce$,,9%), /* Vendor or Job Number       */~
                     part$,              /* Part again                 */~
                     revdate$,           /* Reverse Date               */~
                     procqty,            /* Quantity Procurred         */~
                     proccost,           /* Inventory Cost             */~
                     " ",                /* Filler                     */~
                     str(procsrce$,10%,1%),  /* PO or Invoice (P or I) */~
                     procdate$,          /* Date Ordered               */~
                     stdcost,            /* Current Std Cost           */~
                     str(procsrce$,11%,16%),  /* PO or Invoice Number  */~
                     costtype$           /* Curr Default Cost Method   */


L10410:         FMT  CH(6), CH(25), CH(9), CH(25), CH(6), 2*PD(14,4),    ~
                     CH(15), CH(1), CH(6), PD(14,4), CH(16), CH(1)

            end

