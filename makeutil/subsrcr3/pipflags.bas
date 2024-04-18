        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   IIIII  PPPP   FFFFF  L       AAA    GGG    SSS    *~
            *  P   P    I    P   P  F      L      A   A  G      S       *~
            *  PPPP     I    PPPP   FFFF   L      AAAAA  G GGG   SSS    *~
            *  P        I    P      F      L      A   A  G   G      S   *~
            *  P      IIIII  P      F      LLLLL  A   A   GGG    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPFLAGS - CHECKS THE PIP FOR THREE CONDITIONS-           *~
            *           2, 3 = TOO MUCH SOMEWHERE                       *~
            *           4, 5 = INTRUSION ON SAFETY STOCK                *~
            *           8, 9 = PIP ACTUALLY NEGATIVE                    *~
            *           (PUR = EVEN, MFG = ODD)                         *~
            *           STATUS = BLANK IS A-OK.                         *~
            *            UPDATES PIP W/ QTY, THEN REWRITES THE PIP      *~
            *----------------------------------------------------------Q*~
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
            * 03/04/83 ! ORIGINAL                                 ! GLW *~
            * 03/27/84 ! CHANGE TO SURPLUS CALCULATION            ! ERN *~
            * 04/30/87 ! NEEDS 'REAL' TODAY FOR STATUS CALC.      ! KAB *~
            * 05/06/88 ! Changed Status Calcs To Use Shelf Not Pip! RJM *~
            *          !   & Improved Logic To Avoid Unnecessary  !     *~
            *          !   Operations And Disk I/O                !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

         sub "PIPFLAGS"                                                  ~
                    (part$,              /* THE PART TO CHECK          */~
                     today%,             /* SUBSCRIPT FOR TODAY        */~
                     day%,               /* DAY QUANTITY ADDED         */~
                     qty,                /* QUANTITY TO ADD            */~
                     #2,                 /* PIPMASTR                   */~
                     #41)                /* SFCUM2                     */

        dim part$25, pstat$1, pip%(490), cumf%(490), pqty(2), stat$1

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto    L01950
            cms2v$ = "05.01.00 07/01/88 General Release R5.01.00        "
L01950: REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! System Control File                      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

        REM FIND ACTUAL TODAY FOR STATUS CALCULATION (ONE TIME)

            if atcday% <> 0% then L09000

            call "OPENCHCK" (#1, fs%, 0%, 0%, " ")
               if fs% < 0% then L05800
            call "PIPINDEX" (#1, " ", atcday%, fs%)
               if fs% = 0% then L05900
L05800:           atcday% = 1%
L05900:        close #1

L09000: REM INITIALIZATION

           if today% < 1% or today% > 490% then end
           if day% > 490% then end

           pqty%=int(round(abs(qty),0%))*sgn(qty)
           mat pip% = zer  :  mat cumf% = zer

        REM GET SFCUM2.

           call "READ100" (#41, part$, f1%)
               if f1% = 0% then L20000

           get #41, using L10600, cumf%()
L10600:        FMT POS(26), 490*BI(4)


L20000: REM PIPMASTR AND STATUS SETTING SECTION

           call "READ101" (#2, part$, f1%)
                if f1% = 0% then end

           get #2, using L20600, stat$, pip%(), pqty(), tp%
L20600:         FMT CH(1), POS(27), 490*BI(4), POS(1995), 2*PD(14,4),    ~
                    POS(2019), BI(2)

           pstat$ = " "
           atc%=999999999%
           start% = min(atcday%,max(1%,day%))

           if pqty% = 0% then L21530

           for jjj% =  start% to 490%
               if jjj% >= day% then pip%(jjj%) = pip%(jjj%) + pqty%
               shelf% = pip%(jjj%) - max(0%, cumf%(jjj%))
               if jjj% >= atcday% then atc%=min(atc%,shelf%)
           next jjj%
           goto L21600

L21530:    for jjj% =  atcday% to 490%
               shelf% = pip%(jjj%) - max(0%, cumf%(jjj%))
               atc%=min(atc%,shelf%)
           next jjj%

L21600:    if tp% = 0% or tp% > 499% then L22200
              if atc% < pqty(1) then pstat$ = "5"
              if atc% < 0% then pstat$= "9"
              if atc% > pqty(1) + pqty(2) then pstat$ = "3"
           goto L22600

L22200:       if atc% < pqty(1) then pstat$ = "4"
              if atc% < 0% then pstat$= "8"
              if atc% > pqty(1) + pqty(2) then pstat$ = "2"

L22600:    if pqty% <> 0% or pstat$ <> stat$ then L22650
               call "STRTRLSE" (#2)
               end

L22650:    put #2, using L22700, pstat$, pip%()
L22700:        FMT POS(1), CH(1), POS(27), 490*BI(4)
           rewrite #2
           end
